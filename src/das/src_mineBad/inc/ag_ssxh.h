/* NAME :  ag_ssxh.h          MODULE : Intersection surface/surface
** VERSION : 0.5              DATE LAST MODIFIED : 02/01/88
** CONTAINS: surface/surface intersection structures
**  ag_ssxh      surface/surface intersection header 
**  ag_ssx_ov    surface/surface overlap  
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/

typedef struct ag_ssxh { /* surface/surface  intersection header   */
   struct ag_surface      *srfA;         /* surface                */
   struct ag_surface      *srfB;         /* surface                */
   double                 ftol;          /* fitting tolerance      */
   struct ag_crvs_list    *x_crvs;       /* curves of intersection */
   struct ag_cp_list      *x_pts;        /* isolated pts of int    */
   struct ag_ssx_ov       *x_ov0;        /* overlapping regions    */
   } AG_SSXH, *AG_SSXHP;

typedef struct ag_ssx_ov {  /* surface/surface  overlap            */
   struct ag_cnode        *bspna;        /* cornernodes of bispans */
   struct ag_cnode        *bspnb;        /* cornernodes of bispans */
   struct ag_curve        *ovlp_bndry;   /* overlap boundary curve */
   } AG_SSX_OV, *AG_SSX_OVP;

/*
Notes: ovlp_bndry is a 3d curve which gives the outline of the 
       overlap region on a single pair of bispans.
*/
