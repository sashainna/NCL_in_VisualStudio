/* NAME :  ag_surfaces.h      MODULE : Surface definition
** VERSION : 0.7              DATE LAST MODIFIED : 04/26/88
** CONTAINS: Surface data structures
**  ag_surface   b-spline surface structure 
**  ag_snode     surface node structure  
**  ag_sp_array  spoint array header 
**  ag_spoint    spoint data structure    
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/

typedef struct ag_surface  {    /* b-spline surface structure      */
    int               dim;      /* dimension  (usually 3)          */ 
    int               stype;    /* code for the type of surface    */
    int               mu;       /* u degree                        */
    int               mv;       /* v degree                        */
    int               nu;       /* number of u spans               */
    int               nv;       /* number of v spans               */
    int               ratu;     /* 0(nonrat) 1(rational) -1(homo)   */
    int               ratv;     /* 0(nonrat) 1(rational) -1(homo)  */
    int               formu;    /* u 0(open) 1(closed) 2(periodic) */
    int               formv;    /* v 0(open) 1(closed) 2(periodic) */
    int               poleu;    /* 0(none) 1(u0) 2(un) 3(u0 ,un)   */
    int               polev;    /* 0(none) 1(v0) 2(vn) 3(v0 ,vn)   */
    struct ag_snode   *node0;   /* node for knot (u0, v0)          */
    struct ag_snode   *noden;   /* node for knot (un, vn)          */
    struct ag_snode   *node;    /* current surface node            */
    struct ag_mmbox   *sbox;    /* min-max box                     */
    } AG_SURFACE, *AG_SURFACEP;

typedef struct ag_snode    {    /* surface node structure          */
    struct ag_snode   *unext;   /* next u node                     */
    struct ag_snode   *uprev;   /* previous u node                 */
    struct ag_snode   *vnext;   /* next v node                     */
    struct ag_snode   *vprev;   /* previous v node                 */
    double            *Pw;      /* control point and weight        */
    double            *u;       /* u-knot                          */
    double            *v;       /* v-knot                          */
    } AG_SNODE, *AG_SNODEP;

/* *** SURFACE POINTS *** */ 
typedef struct ag_sp_array {    /* spoint array header             */
    int               dim;      /* dimension of points             */
    int               nu;       /* number of u spoints             */
    int               nv;       /* number of v spoints             */
    struct ag_spoint  *sp1;     /* lower left spoint               */
    struct ag_spoint  *spn;     /* upper right spoint              */
    } AG_SP_ARRAY, *AG_SP_ARRAYP;

typedef struct ag_spoint   {    /* spoint data structure           */
    struct ag_spoint  *unext;   /* next u spoint                   */
    struct ag_spoint  *uprev;   /* previous u spoint               */
    struct ag_spoint  *vnext;   /* next v spoint                   */
    struct ag_spoint  *vprev;   /* previous v spoint               */
    double            *P;       /* point                           */
    } AG_SPOINT, *AG_SPOINTP;


