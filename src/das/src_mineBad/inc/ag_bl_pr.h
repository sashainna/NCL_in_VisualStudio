/* NAME :  ag_bl_pr.h         MODULE : Curve definition
** VERSION : 0.5              DATE LAST MODIFIED : 02/01/88
** CONTAINS: Blend, point and radius data
**  ag_bl_Prh      blend, point, radius header
**  ag_bl_Prd      blend, point, radius data 
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/

typedef struct ag_bl_Prh   {    /* blend, point, radius header     */
   int                n;        /* number of data points           */
   int                dim;      /* dimension of points             */
   struct ag_bl_Prd   *Prd1;    /* first data point                */
   struct ag_bl_Prd   *Prdn;    /* last data point                 */
   } AG_BL_PRH, *AG_BL_PRHP;

typedef struct ag_bl_Prd   {    /* blend, point, radius data       */
   struct ag_bl_Prd   *next; 
   struct ag_bl_Prd   *prev;
   int                bl_type;  /* blend type 0, 1 or 2            */
   double             *P;       /* point                           */
   double             r0,r1;    /* offset radii                    */
   double             rho;      /* for a rho conic or cubic        */
   } AG_BL_PRD, *AG_BL_PRDP;

/*
Notes: These structures are used to hold the data required to blend
       together line segments, used by routine ag_crv_line_bl.

       1. The list of points is a loop, and define a closed curve,
         if Prd1->prev = Prdn and Prdn->next = Prd1.
       2. If r0 <= 0, there is no blend segment at this point.
       3. If r0 > 0 and
          bl_type = 0: if rho <= 0 
                         if r0 = r1 blend is a circle
                         else blend is an ellipse
                      else blend is a rho conic
                 = 1: if rho <= 0 blend is a cubic
                      else blend is a rho cubic
                 = 2: if rho <= 0 
                         blend is a zero curvature cubic
                      else
                         blend is a zero curvature rho cubic
          Let theta be the exterior angle between the next and
          previous line segments at a point.
       4. If r0 and r1 are given, r0 and r1 must satisfy
          cos(theta) < r0/r1 and cos(theta) < r1/r0. (Which is
          always true if r0 = r1 or cos(theta) <= 0 ie.theta > PI/2.)
       5. If theta = 0 there is no blend segment.
       6. At points Pi_1 and Pi, r0, r1 and theta must not result
          in line segment with the opposite direction as Pi_1 to Pi.
*/
                         
