/* NAME :  ag_csx_work.h      MODULE : Intersection curve/surface 
** VERSION : 0.5              DATE LAST MODIFIED : 02/01/88
** CONTAINS: curve/surface intersection working structures
**  ag_sub_pat     sub patch data structure   
**  ag_spattn      sub patch tree node 
**  ag_bis_tnd     (sub) bi-span tree node
**  ag_pancake     "thickened" parallelogram 
**  ag_cs_stkn     curve surface stack node
**  ag_sbx_dat     span bi-span intersection data
**  ag_sb_stkn     span bi-span stack node
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/
 
/* *** SUB PATCH STRUCTURES *** */
 
typedef struct ag_sub_pat {     /* sub patch data structure        */
   struct ag_snode *S;          /* lower corner node of subpatch   */
   int      nu;                 /* number of nontrivial u spans    */
   int      nv;                 /* number of nontrivial v spans    */
   } AG_SUB_PAT, *AG_SUB_PATP;
 
typedef struct ag_spattn {      /* sub patch tree node             */
   struct ag_sub_pat      *spat;
   struct ag_mmbox        *bx;
   struct ag_spattn       *parent;
   struct ag_spattn       *child[2];
   int                  count;
   } AG_SPATTN, *AG_SPATTNP;
 
/* *** SUB BI-SPAN STRUCTURES *** */

typedef struct ag_bis_tnd {    /* (sub) bi-span tree node          */
  double                   u[2],v[2];
  int                      count;
  struct ag_bis_tnd        *parent;
  struct ag_bis_tnd        *child[4];
  struct ag_pancake        *cake;
  } AG_BIS_TND, *AG_BIS_TNDP;
 
typedef struct ag_pancake {    /* "thickened" parallelogram        */
  double              P[3];    /* corner point                     */
  double         U[3],V[3];    /* directions of two of the axies   */
  double              N[3];    /*  " of 3rd axis normal to U & V   */
  double               a,b;    /* lengths of the U and V axies     */
  double                 r;    /* radial thickness                 */
  double              sin2;    /* max dev^2 of normal ang from N   */
  } AG_PANCAKE, *AG_PANCAKEP;

/* *** CSX STRUCTURES  (curve surface) *** */
 
typedef struct ag_cs_stkn {       /* curve surface stack node      */
  struct ag_cs_stkn        *next;
  struct ag_scrvtn         *ctn;  /* curve tree node               */
  struct ag_spattn         *ptn;  /* patch tree node               */
  } AG_CS_STKN, *AG_CS_STKNP;
 

/* *** SBX STRUCTURES  (span bi-span) *** */
 
typedef struct ag_sbx_dat {      /* span bi-span intersection data */
  struct ag_csxh        *H;      /* header struc for intersection  */
  struct ag_scrvtn      *Tc;     /* subcurve tree node             */
  struct ag_spattn      *Tp;     /* patch tree node                */
  struct ag_sb_stkn     *S;      /* span bi-span stack             */
  struct ag_spline      *bez1;   /* bezier span                    */
  struct ag_spline      *pow1;   /* power span                     */
  struct ag_surface     *bez2;   /* bezier patch                   */
  struct ag_surface     *pow2;   /* power patch                    */
  struct ag_capsule     *cap;    /* capsule for span               */
  struct ag_pancake     *cake;   /* pancake for bi-span            */
  } AG_SBX_DAT, *AG_SBX_DATP;
 
typedef struct ag_sb_stkn {      /* span bi-span stack node        */
  struct ag_sb_stkn     *next;
  struct ag_spn_tnd     *spnt;   /* (sub) span tree node          */
  struct ag_bis_tnd     *bist;   /* (sub) bi_span tree node       */
  } AG_SB_STKN, *AG_SB_STKNP;
 
/*
Notes: The sub_pat structure is used as follows:
       1) if S == NULL then we have a subset of n splines
           starting with spline B;
       2) if S != NULL then we have a subset of non trivial
           spans of B starting with the one at node S;
       3) sn is always set to the sequence number of B in
           the curve containing B. sn = 1, 2, ..., nbs.
       4) if n <= 1 then we have one span thus values from
           0 on down can be used to pass info about the span.
 
        The sbxdata structure must be initialized as follows:
        1) bez1 and pow1 allocated for maxdegree
        2) bez2, pow2, work1, work2, work3 allocated for
           maxdeg X maxdeg.
        3) Hw, cap, and cake should be allocated
        4) all other pointers set to NULL;
*/
