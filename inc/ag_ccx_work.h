/* NAME :  ag_ccx_incl.h      MODULE : Intersection curve/curve 
** VERSION : 0.5              DATE LAST MODIFIED : 02/01/88
** CONTAINS: curve/curve intersection working structures
**  ag_sub_crv     sub curve data structure   
**  ag_scrvtn      sub curve tree node         
**  ag_spn_tnd     (sub) span tree node
**  ag_capsule     all points within range of seg P+tU
**  ag_cc_stkn     curve curve stack node
**  ag_ss_stkn     span span stack node
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/

/* *** SUBCURVE STRUCTURES *** */
 
typedef struct ag_sub_crv {        /* sub curve data structure     */
   struct ag_spline   *B;          /* starting bspline             */
   struct ag_cnode    *S;          /* NULL for subcurve,else start 
                                      of first span for sub spline */
   int                n;           /* number of spans/splines      */
   int                sn;          /* spline sequence number       */
   } AG_SUB_CRV, *AG_SUB_CRVP;
 
typedef struct ag_scrvtn {         /* sub curve tree node          */
   struct ag_sub_crv  *scrv;
   struct ag_mmbox    *bx;
   struct ag_scrvtn   *parent;
   struct ag_scrvtn   *child[2];
   int                count;
   } AG_SCRVTN, *AG_SCRVTNP;
                        
/* *** SUBSPAN STRUCTURES *** */

typedef struct ag_spn_tnd {        /* (sub) span tree node         */
  double              t0, t1;
  int                 count;
  struct ag_spn_tnd   *parent;
  struct ag_spn_tnd   *child[2];
  struct ag_capsule   *cap;
  } AG_SPN_TND, *AG_SPN_TNDP;
 
typedef struct ag_capsule {        /* all points w/in r of seg P+tU*/
  double              P[3];        /* base point                   */
  double              U[3];        /* unit direction               */
  double              len;         /* length                       */
  double              r;           /* radius                       */
  double              sin2;        /* max sinsq angle of crv with U*/
  } AG_CAPSULE, *AG_CAPSULEP;
 
/* *** CCX STRUCTURES *** */
 
typedef struct ag_cc_stkn {         /* curve curve stack node      */
  struct ag_cc_stkn   *next;                                        
  struct ag_scrvtn    *ctn0;        /* curve tree node 0           */
  struct ag_scrvtn    *ctn1;        /* curve tree node 1           */
  } AG_CC_STKN, *AG_CC_STKNP;        

/* *** SSX STRUCTURES *** */
                              
typedef struct ag_ss_stkn {         /* span span stack node        */
  struct ag_ss_stkn   *next;                                       
  struct ag_spn_tnd   *ssp0;        /* curve tree node 0           */
  struct ag_spn_tnd   *ssp1;        /* curve tree node 1           */
  } AG_SS_STKN, *AG_SS_STKNP;
 
typedef struct ag_ssx_dat {         /* span span intersection data */
  struct ag_ccxh      *H;           /* curve x curve header        */
  struct ag_ss_stkn   *S;           /* pointer to top of stack     */
  struct ag_spline    *bez0,*bez1;  /* spans in Bezier form        */
  struct ag_spline    *pow0,*pow1;  /* spans in power basis form   */
  struct ag_capsule   *cap0,*cap1;  /* capsules of spans           */
  double              *closure_ptr; /* if curve not closd then NULL
                                       else closure_pt (below).    */
  double              closure_pt[3];/* initial point of curve      
                                       provided curve is closed    */
  int                 dim;
  } AG_SSX_DAT, *AG_SSX_DATP;
 
/*
Notes:  The ag_sub_crv structure is used as follows:
        1) if S == NULL then we have a subset of n splines
           starting with spline B;
        2) if S != NULL then we have a subset of non trivial
           spans of B starting with the one at node S;
        3) sn is always set to the sequence number of B in
           the curve containing B. sn = 1, 2, ..., nbs.
        4) if n <= 1 then we have one span thus values from
           0 on down can be used to pass info about the span.
*/
