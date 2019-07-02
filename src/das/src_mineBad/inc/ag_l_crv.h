/* NAME :  ag_l_crv.h          MODULE : Curve internal
** VERSION : 0.5              DATE LAST MODIFIED : 02/01/88
** CONTAINS: Length of curve data structures
**  ag_l_crv_h     length of curve data header
**  ag_l_bs_d      length of b-spline data 
**  ag_l_sp_d      length of span data 
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/

typedef struct ag_l_crv_h {         /* length of curve data header */
   struct ag_curve         *crv;
   double                  eps;     /* tolerance                   */
   double                  length;  /* length                      */
   struct ag_l_bs_d        *l_bs0;  /* first b-spline data         */
   } AG_LCRV, *AG_LCRVP;

typedef struct ag_l_bs_d {          /* length of b-spline data     */
   struct ag_l_bs_d        *next;   /* next and previous bs data   */
   struct ag_l_bs_d        *prev;   /* next and previous bs data   */
   struct ag_spline        *bs;
   double                  eps;     /* tolerance                   */
   double                  length;  /* length                      */
   struct ag_l_sp_d        *l_sp0;  /* first span data             */
   } AG_LBSPL, *AG_LBSPLP;

typedef struct ag_l_sp_d {         /* length of span data          */
   struct ag_l_sp_d        *next;
   struct ag_l_sp_d        *prev;
   struct ag_cnode         *node;  /* span pointer                 */
   double                  eps;    /* tolerance                    */
   double                  length; /* length                       */
   } AG_LSPAN, *AG_LSPANP;

/*
Notes: These structures are used to hold the arc length of curve data
       as calculated by ag_leng_crv and are used by the other 
       functions that use length of curve.
*/ 
