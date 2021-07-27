
/*********************************************************************
**    NAME         :  m3ecirc4.c
**       CONTAINS:
**       um_lnatdist
**       um_cir_ttr
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3ecirc4.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:51
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "mdrel.h"
#include "mdgenent.h"
#include "mcrv.h"
#include "mdcpln.h"
#include "modef.h"
#include "misect.h"

/*********************************************************************
**    E_FUNCTION     : um_lnatdist(nvec, dist, lin, lout)
**       Create a line entity which is parallel to the given
**       line entity, lies in a plane normal to the given normal, 
**         and is at a specified distance.
**    PARAMETERS   
**       INPUT  : 
**            nvec            normal vector
**          dist            distance from given line
**          lin             original line
**       OUTPUT :  
**            lout            parallel line at distance in plane
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_lnatdist(nvec, dist, lin, lout)
   UM_vector nvec;
   UM_length dist;
   struct UM_line_rec  *lin;
   struct UM_line_rec  *lout;

   {
   UM_vector vec;            /* vector normal to line and plane
                              containing line */

   uu_denter( UU_MTRC,(us,"um_lnatdist((%g,%g,%g),%g,%8x,?)",
      nvec[0],nvec[1],nvec[2],dist,lin));
   um_vcmnvc(lin->ept, lin->spt, vec);
   um_cross(vec, nvec, vec);
   um_unitvc(vec, vec);
   um_vctmsc(vec, dist, vec);
   ur_setup_data(UM_LINE_REL, lout, sizeof(struct UM_line_rec));
   /* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
   strcpy (lout->label, "");
   lout->subscr = 0;
   lout->key = 0;
   um_vcplvc(lin->spt, vec, lout->spt);
   um_vcplvc(lin->ept, vec, lout->ept);
   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : um_cir_ttr(fillet, ploc, e, radius, c, npt, err)
**       Calculate the parameters defining a circular arc which 
**       is tangent to line/circle, circle/line, or circle/circle
**         and of a specified radius. The lines/circles must be coplanar.
**    PARAMETERS   
**       INPUT  : 
**            fillet       UU_TRUE => fillet operation
**          ploc            picked location on entities and
**                           approximate circle center if
**                           circle/circle tangent
**          e               two entities picked
**          radius          specified radius
**       OUTPUT :  
**            c               new circular arc
**          npt             point of tangency on picked entities
**          err             error flag
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_cir_ttr(fillet, ploc, e, radius, c, npt, err)
   UU_LOGICAL fillet;
   UD_NDCLOCREC ploc[3];
   struct UM_crvdatabag e[2];
   UM_length radius;
   struct UM_circle_rec  *c;
   UU_REAL npt[2][3];
   UU_LOGICAL  *err;

   {
   UU_REAL temppt[2][3];         /* temporary points */
   UU_REAL tempvec[3];            /* temporary vector */
   UU_REAL aproxcnt[3];            /* world coordinate of aproximate center */
   UU_REAL pickpt[2][3];         /* world coordinate of picked points */
   int n[2];                     /* number of entities to intersect while
                                    looking for tangent circle */
   struct UM_crvdatabag te[2][2];/* entitites to intersect while looking
                                    for tangent circle */
   struct UM_circle_rec *ptr;
   int nint;                     /* number of intersection points */
   int tnint;                     /* total number of intersection points */
   UM_isect ibuff[8];            /* all intersection points */
   UU_REAL tanpt[2][3];            /* tangency points of circle and
                                    given entities */
   int closest;                  /* index into ibuff for closest intersection */
   UU_REAL ulvec[2][3];          /* unit vector to nearest point */
   UU_REAL nvec[3];              /* unit normal to plane of curves */
   UU_REAL signtest;             /* used to test sign of cross product */
   int i, j, ic, il;             /* indicies */
   int numcircles;               /* number of circle entities */
   UU_LOGICAL reject;            /* UU_TRUE iff circle is not possible
                                    choice for tangent circle */
   UU_REAL clinept[3];           /* point defining critical line */
   UU_REAL clinevec[3];          /* vector defining critical line */
   UU_REAL testvec[3];           /* vector between two tangent points */
   UU_REAL mindist;              /* distance of intersection point to picked 
                                    locations */
   UU_REAL r,dist0,dist1,dist, sirad[2];   /* used to calculate mindist */
   UM_coord zpoint,nlpt, tpic[2], cicr[2];
   UM_vector vc, vcc, vcl, pvec, ccvc[2];
   UU_REAL um_dploccc();

   uu_denter(UU_MTRC,(us,"um_cir_ttr(?,?,?,?,?,?,?)"));

   um_pscroll("um_cir_ttr: first entity");
   uc_print(&e[0]);
   um_pscroll("um_cir_ttr: second entity");
   uc_print(&e[1]);

/* 
...check if entities are coplanar 
*/
   *err = !um_ents_coplanar(&e[0], UM_DEFAULT_TF, &e[1], UM_DEFAULT_TF);
   if (*err)
      {
      uu_uerror0(/*entities are not coplanar*/UM_MODEL,31);
      goto done;
      }
   i    = 0;
   if (e[1].rel_num == UM_CIRCLE_REL) i = 1;
   ptr = (struct UM_circle_rec *) &e[i];
   um_vctovc(ptr->center,zpoint);
   um_vctovc(ptr->nvec,nvec);
   um_projploctopln(&ploc[0],zpoint,nvec,pickpt[0]);
   um_projploctopln(&ploc[1],zpoint,nvec,pickpt[1]);

/* 
...determine a normal to the plane containing the entities 
...and possibly a critical line used to select the desired
...tangent circle 
*/
   ic    = 2;
   if ( (e[0].rel_num ==  UM_CIRCLE_REL) 
         && (e[1].rel_num ==  UM_CIRCLE_REL) )
      {
      struct UM_circle_rec *ptr0,*ptr1;
      ptr0 = (struct UM_circle_rec *) &e[0];
      ptr1 = (struct UM_circle_rec *) &e[1];
      um_vctovc(ptr0->center,clinept);
      um_vctovc(clinept,cicr[0]);
      um_vctovc(ptr1->center,cicr[1]);
      um_vcmnvc(cicr[0],cicr[1],clinevec);
      um_projploctopln(&ploc[2],zpoint,nvec,aproxcnt);
      for (j=0; j<2; j++)
        {
         um_vcmnvc (aproxcnt,cicr[j],ccvc[j]);
         um_vcmnvc (pickpt[j],clinept,tpic[j]);
         um_cross (tpic[j],clinevec,vcc);
         um_cross (ccvc[j],clinevec,vcl);
         sirad[j] = (um_dot(vcc,vcl) > 0.0)? 1.0: -1.0;
        } 
      numcircles = 2;
      }
   else if ((e[0].rel_num == UM_CIRCLE_REL) 
            && (e[1].rel_num == UM_LINE_REL))
      ic   = 0; 
   else if ((e[1].rel_num ==  UM_CIRCLE_REL) 
            && (e[0].rel_num == UM_LINE_REL))
      ic   = 1;
   else
      {
      *err = UU_TRUE;
      uu_uerror0(/*um_cir_ttr: illegal entities*/UM_MODEL,32);
      goto done;
      }
   if (ic < 2)
     {
      struct UM_circle_rec *ptr0;
      struct UM_line_rec *ptr1;
      il   = 1 - ic;
      ptr0 = (struct UM_circle_rec *) &e[ic];
      ptr1 = (struct UM_line_rec *) &e[il];
      um_vctovc(ptr0->nvec,nvec);
      um_vctovc(ptr0->center,clinept);
      um_vctovc(clinept,cicr[ic]);
      um_vcmnvc(ptr1->ept, ptr1->spt, ulvec[il]);
      um_unitvc(ulvec[il], ulvec[il]);
      um_nptln(clinept, ptr1->spt, ulvec[il], tanpt[il]);
      um_vcmnvc(tanpt[il],clinept,clinevec);
      um_nptln (pickpt[ic],ptr1->spt,ulvec[il],nlpt);
      um_vcmnvc (pickpt[ic],nlpt,pvec);      
      um_unitvc (pvec,pvec);
      if (um_mag(clinevec) < UM_FUZZ)
         um_vctovc (pvec,clinevec);
      um_vctmsc (pvec,radius,pvec);
      um_vcmnvc (pickpt[ic],clinept,vcc);
      um_unitvc(vcc, vcc);
      um_vcmnvc (pickpt[il],clinept,vcl);
      um_unitvc(vcl, vcl);
      um_cross (vcl,clinevec,vc);
      um_cross (clinevec,vcc,vcc);
      sirad[ic] = (um_dot (vcc,vc) > 0.0)? -1.0: 1.0; 
      numcircles = 1;
     }

   um_unitvc(clinevec,clinevec);
/* 
...determine geometry necessary to calculate all possible tangent
...circles to the two specified entities which has the given radius
*/
   for (i = 0; i < 2; i++)
      {
      switch (e[i].rel_num)
         {
         case  UM_LINE_REL:
           {
            struct UM_line_rec *eptr;
            eptr = (struct UM_line_rec *) &e[i];
            um_vcplvc (eptr->spt,pvec,nlpt);
            um_c2_ptvec (nlpt,ulvec[i],&te[0][i]);
            n[i] = 1;
           }
            break;
         case  UM_CIRCLE_REL:
            {
            struct UM_circle_rec *eptr;
            struct UM_circle_rec *teptr0, *teptr1;
            eptr = (struct UM_circle_rec *) &e[i];
            teptr0 = (struct UM_circle_rec *) &te[0][i];
            teptr1 = (struct UM_circle_rec *) &te[1][i];
            um_vctovc(eptr->nvec, nvec);
            um_c3_copy(&e[i], &te[0][i]);
 
            dist = um_dcccc (pickpt[1-i],cicr[i]);
            if (radius < eptr->radius)
               {
                r = (dist > eptr->radius)? radius: -radius;
                teptr0->radius = teptr0->radius + r;
               }
            else
               {
                teptr0->radius = radius + sirad[i]*teptr0->radius;
               }
            n[i] = 1;
/*
              if (radius > (eptr->radius - UM_FUZZ) )
               {
               teptr1->radius = radius - teptr1->radius;
               }
            else
               {
               teptr1->radius = teptr1->radius - radius;
               }
*/
            }
            break;
         default:
            uu_uerror0(/*you must pick a line or circle*/UM_MODEL,33);
            *err = UU_TRUE;
            goto done;
            break;
         }
      }

/* 
...determine the center point of all possible tangent circles by 
...intersecting circles (lines) offset from the picked entities 
*/
   tnint = 0;
   for (i = 0; i < n[0]; i++)
      {
      for (j = 0; j < n[1]; j++)
         {
         um_isect(&te[i][0], UM_DEFAULT_TF, &te[j][1], UM_DEFAULT_TF, &nint,
            UM_MAXISECT, &ibuff[tnint]);
         tnint = tnint + nint;
         }
      }
   if (tnint == 0)
      { /*can not calculate a circle tangent to specified entities*/
      *err = UU_TRUE;
      uu_uerror0(UM_MODEL,34);
      goto done;
      }

/* 
...from all of the circles defined above, determine the one specified
...by the user 
*/
   c->radius = radius;
   c->dang = UM_TWOPI;
   um_vctovc(UM_cpln.zaxis,c->nvec);
   um_vctovc(UM_cpln.yaxis,c->svec);
   closest = -1;
   for (i = 0; i < tnint; i++)
      {
      um_vctovc(ibuff[i].pt,c->center);
/* 
...calculate the points of tangency between the potential
...tangent circle and the picked entities 
*/
      for (j = 0; j < 2; j++)
         {
         switch (e[j].rel_num)
            {
            case  UM_LINE_REL:
               {
               struct UM_line_rec *ptr;
               ptr = (struct UM_line_rec *) &e[j];
               um_vcmnvc(ptr->ept, ptr->spt, ulvec[j]);
               um_unitvc(ulvec[j], ulvec[j]);
               um_nptln(ibuff[i].pt, ptr->spt, ulvec[j], tanpt[j]);
               }
               break;
            case  UM_CIRCLE_REL:
               {
               struct UM_circle_rec *ptr;
               ptr = (struct UM_circle_rec *) &e[j];
               um_vcmnvc(ptr->center, ibuff[i].pt, ulvec[j]);
               um_unitvc(ulvec[j], ulvec[j]);
               um_vctmsc(ulvec[j], radius, tempvec);
               um_vcplvc(ibuff[i].pt, tempvec, temppt[0]);
               um_vcmnvc(ibuff[i].pt, tempvec, temppt[1]);
               dist0 = um_dcccc(ptr->center,temppt[0]);
               dist0 = fabs(ptr->radius - dist0);
               dist1 = um_dcccc(ptr->center,temppt[1]);
               dist1 = fabs(ptr->radius - dist1);
               if (dist0 < dist1   )
                  um_vctovc(temppt[0],tanpt[j]);
               else
                  um_vctovc(temppt[1],tanpt[j]);
               }
               break;
            }
         }
/* 
...for circle/circle tangency, circle center must lie on
...same side of line between tangent points as approximate center 
*/
      reject = UU_FALSE;
      if (numcircles == 2)
         {
         um_vcmnvc(tanpt[1],tanpt[0],testvec);
         um_vcmnvc(ibuff[i].pt,tanpt[0],ulvec[0]);
         um_vcmnvc(aproxcnt,tanpt[0],ulvec[1]);
         um_cross(testvec,ulvec[0],ulvec[0]);
         um_cross(testvec,ulvec[1],ulvec[1]);
         signtest = um_dot(ulvec[0],ulvec[1]);
         reject = (signtest < 0.0);
         }
/* 
   for circle/circle tangency or circle/line tangency,
   the tangent points must be on the same side of the
   critical line as the point used to pick the entity 
*/
/*   vp 4-apr-95, test removed since fillet is generated using 
     pick point coordinates

      if ((!reject) && (numcircles >= 1))
         {
         um_vcmnvc(pickpt[0],clinept,ulvec[0]);
         um_cross(ulvec[0],clinevec,ulvec[0]);
         um_vcmnvc(tanpt[0],clinept,ulvec[1]);
         um_cross(ulvec[1],clinevec,ulvec[1]);
         signtest = um_dot(ulvec[0],ulvec[1]);
         reject = (signtest < 0.0);
         if (!reject)
            {
            um_vcmnvc(pickpt[1],clinept,ulvec[0]);
            um_cross(ulvec[0],clinevec,ulvec[0]);
            um_vcmnvc(tanpt[1],clinept,ulvec[1]);
            um_cross(ulvec[1],clinevec,ulvec[1]);
            signtest = um_dot(ulvec[0],ulvec[1]);
            reject = (signtest < 0.0);
            }    
         }      
*/
/* 
...for all cases, the tancentry points must pass the
...minimum distance check 
*/
      if (!reject)
         {
         dist0 = um_dcccc(pickpt[0], tanpt[0]);
         dist1 = um_dcccc(pickpt[1], tanpt[1]);
         dist = dist0 + dist1;
         if ((closest == -1) || (dist < mindist))
            {
            mindist = dist;
            closest = i;
            um_vctovc(tanpt[0],npt[0]);
            um_vctovc(tanpt[1],npt[1]);
            }
         }
      }

   if (closest == -1)
      {
      *err = UU_TRUE;
      uu_uerror0(/*can not calculate a circle tangent to specified entities*/
               UM_MODEL,34);
      goto done;
      }
/* 
...define the tangent circle 
*/
   c->rel_num =  UM_CIRCLE_REL;
   c->radius = radius;
   um_vctovc(ibuff[closest].pt, c->center);
   for (i=0; i<2; i++)
      {
      um_vcmnvc(npt[i], c->center, ulvec[i]);
      um_unitvc(ulvec[i], ulvec[i]);
      }
   um_vctovc(ulvec[0], c->svec);
   um_cross(ulvec[0], ulvec[1], c->nvec);
   if (um_mag(c->nvec) < UM_FUZZ)
       um_vctovc (nvec,c->nvec);
   um_unitvc(c->nvec, c->nvec);
   c->dang = um_angle2p(ulvec[0], ulvec[1],c->nvec);
/*
...vp 30-mar-95 drop this test, use true angles
*/
/*
   c->dang = um_angle(ulvec[0], ulvec[1]);
   um_cross(ulvec[0], ulvec[1], c->nvec);
   signtest = um_dot(c->nvec,  UM_cpln.zaxis);
   if (signtest < 0)
      {
      for (i = 0; i < 3; i++)
         c->nvec[i] = -c->nvec[i];
      c->dang = UM_TWOPI - c->dang;
      }  
   um_unitvc(c->nvec, c->nvec); 
*/

done:;
   uu_dexit;
   }
