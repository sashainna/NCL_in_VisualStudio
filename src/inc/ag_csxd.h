/* NAME :  ag_csxd.h          MODULE : Intersection curve/surface
** VERSION : 0.6              DATE LAST MODIFIED : 03/01/88
** CONTAINS: curve/surface intersection structures
**  ag_csxh        curve/surface intersection data header
**  ag_cfsd        curve/surface intersection data  
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/

typedef struct ag_csxh {       /* crv-srf intersection header      */
   struct ag_curve     *crv;   /* curve                            */
   struct ag_surface   *srf;   /* surface                          */
   struct ag_csxd      *csxd0; /* initial intersection data        */
   } AG_CSXH, *AG_CSXHP;    

typedef struct ag_csxd {       /* crv/srf intersection data        */
   struct ag_csxd      *next;  /* data of loop, ordered by         */
   struct ag_csxd      *prev;  /*               t value on curve   */
   double              uv[2];  /* srf(u,v) = intersect point       */
   double              t;      /* crv(t) = intersect point         */
   int                 c_on_s; /* start of curve on surface ( T/F )*/
   } AG_CSXD, *AG_CSXDP;      

/*
Notes: If c_on_s = 1  then this point is the start of an 
       interval of the curve which is on the surface.
       The end of the interval is the next point.
*/
