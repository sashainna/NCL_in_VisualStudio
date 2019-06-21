/* NAME :  ag_xss_pt.h        MODULE : Intersect internal 
** VERSION : 0.7              DATE LAST MODIFIED : 04/01/88
** CONTAINS:  Points from intersection
**  ag_pth     intersection header
**  ag_pts     pointset header
**  ag_pt      points data 
Copyright (c) 1988 Applied Geometry Corporation. All rights reserved.
********************************************************************/

typedef struct ag_xss_pth  {        /* intersection pointset heade */
   struct ag_xss_pts     *pset0;    /* initial pointer to pointset */
   } AG_XSS_PTH, *AG_XSS_PTHP;

typedef struct ag_xss_pts {         /* pointset header             */
   struct ag_xss_pts     *next;     /* in loop of pointsets        */
   struct ag_xss_pts     *prev;     /* in loop of pointsets        */
   struct ag_xss_pt      *pt0;      /* start point of pointset     */
   struct ag_xss_pt      *ptn;      /* end   point of pointset     */
   } AG_XSS_PTS, *AG_XSS_PTSP;            

typedef struct ag_xss_pt  {        /* point intersection data      */
   struct ag_xss_pt     *next;     /* list                         */
   struct ag_xss_pt     *prev;
   double               P[3];     /* intersection point            */
   double               uva[2];   /* uv value for surface a        */
   double               uvb[2];   /* uv value for surface b        */
   } AG_XSS_PT, *AG_XSS_PTP;

/*
Notes:  from surface surface intersection, points of intersection
        are saved ( P uva, uvb) in sets (from spans).
        Sets can be joined together if they have matching end
        data with start of next data ( span ends idenical) and 
        duplicate start or ends can be removed. Should end with 
        only as many sets as there are curves of intersection.
*/
                                   
