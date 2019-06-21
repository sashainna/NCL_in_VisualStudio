/* NAME :  ag_crv_def.h       MODULE : Curve defining data
** VERSION : 0.7              DATE LAST MODIFIED : 03/12/88
** CONTAINS: Curve defining data structures
**  ag_bez_data  defining data for Bezier 
**  ag_con_data  defining data for conicc
**  ag_cir_data  defining data for circle
**  ag_ell_data  defining data for ellipse
**  ag_prb_data  defining data for parabola
**  ag_crv_data  defining data for all curves
Copyright (c) 1988 Applied Geometry Corporation. All rights reserved.
********************************************************************/

/* *** CURVE DEFINING DATA *** */

typedef struct ag_crv_data  {    /* defining data  for all curves  */
    int                ctype;         /* conic type                */
    int                dim;           /* dimension                 */
    struct ag_spline   *bs;           /* pointer to bspline        */
    struct ag_cnode    *node;         /* pointer to node           */
    double             normal[3];
    double             P0[4];
    double             P1[4];
    double             P2[4];
    double             U[3];
    double             V[3];
    double             w0,w1,w2;
    double             a1,a2,a3,a4,a5;
    double             t1,t2; 
    double             b,b1,b2;
    double             s0,s1;
    double             dscrm;
    } AG_CRV_DATA, *AG_CRV_DATAP;

/* *** CONIC DEFINING DATA *** */

typedef struct ag_cir_data {         /* defining data for circle   */
    int                dim;          /* dimension                  */
    struct ag_spline   *bs;          /* pointer to bspline         */
    struct ag_cnode    *node;        /* pointer to node            */
    double             normal[3]; 
    double             center[3];
    double             radius;
    } AG_CIR_DATA, *AG_CIR_DATAP; 

typedef struct ag_ell_data {        /* defining data for ellipse  */
    int                dim;         /* dimension                  */
    struct ag_spline   *bs;         /* pointer to bspline         */
    struct ag_cnode    *node;       /* pointer to node            */
    double             normal[3];
    double             center[3];
    double             focus1[3];
    double             focus2[3];
    double             major_axis_int[3];
    double             minor_axis_int[3];
    double             major_axis_len;
    double             minor_axis_len; 
    } AG_ELL_DATA, *AG_ELL_DATAP; 

typedef struct ag_prb_data {         /* defining data for parabola */
    int                dim;          /* dimension                  */
    struct ag_spline   *bs;          /* pointer to bspline         */
    struct ag_cnode    *node;        /* pointer to node            */
    double             normal[3];  
    double             vertex[3];
    double             focus[3];
    } AG_PRB_DATA, *AG_PRB_DATAP; 

typedef struct ag_hyp_data {         /* defining data for hyperbola*/
    int                dim;          /* dimension                  */
    struct ag_spline   *bs;          /* pointer to bspline         */
    struct ag_cnode    *node;        /* pointer to node            */
    double             normal[3];
    double             center[3];    
    double             focus[3];    
    double             active_branch_vertex[3];
    double             semi_axis_len;
    double             pt_on_asym1[3];
    double             pt_on_asym2[3];  
    } AG_HYP_DATA, *AG_HYP_DATAP;

typedef struct ag_con_data   {        /* defining data for conic   */  
    int                ctype;         /* conic type                */
    union {                           /* union of conic structs    */
      struct ag_cir_data  circle;
      struct ag_ell_data  ellipse;
      struct ag_prb_data  parabola; 
      struct ag_hyp_data  hyperbola;
      }                   conic_data;
    } AG_CON_DATA, *AG_CON_DATAP; 
  

