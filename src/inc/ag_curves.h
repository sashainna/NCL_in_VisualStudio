/* NAME :  ag_curves.h       MODULE : Curve and Point definition
** VERSION : 0.7              DATE LAST MODIFIED : 04/25/88
** CONTAINS: Curve and point data structures
**  ag_cpl_list  header for list of cp_lists
**  ag_cp_list   cpoint list     
**  ag_cpoint    cpoint data structure
**  ag_curve     curve data structure   
**  ag_spline    b-spline data structure  
**  ag_cnode     curve node data structure
**  ag_mmbox     min-max box data structure  
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/

/* *** POINT DATA *** */
typedef struct ag_cpl_list {    /* header for list of cp_lists     */
    int               n;        /* number of lists                 */
    struct ag_cp_list *cpl1;    /* first cp list                   */
    struct ag_cp_list *cpln;    /* last cp list                    */
    struct ag_mmbox   *cplbox;  /* min-max box of cpl list         */
    } AG_CPL_LIST, *AG_CPL_LISTP;

typedef struct ag_cp_list {     /* cpoint list                     */
    struct ag_cp_list *next;    /* next cp list                    */
    struct ag_cp_list *prev;    /* prev cp list                    */
    int               dim;      /* dimension                       */
    int               n;        /* number of cpoints in list       */
    struct ag_cpoint  *cp1;     /* first cpoint                    */
    struct ag_cpoint  *cpn;     /* last cpoint                     */
    struct ag_mmbox   *cpbox;   /* min-max box of cpoints          */
    } AG_CP_LIST, *AG_CP_LISTP;
 
typedef struct ag_cpoint {      /* cpoint data structure           */
    struct ag_cpoint  *next;    /* next cpoint                     */
    struct ag_cpoint  *prev;    /* previous cpoint                 */
    double            *P;       /* point data                      */
    } AG_CPOINT, *AG_CPOINTP;

/* ***CURVE DATA *** */
typedef struct ag_crvs_list { /* list of curves header             */
   int                  n;       /* number of curves               */
   struct ag_crvs       *crvs0;  /* first in loop of curves        */
   struct ag_crvs       *crvs;   /* current curve                  */
   } AG_CRVS_LIST, *AG_CRVS_LISTP;

typedef struct ag_crvs  { /* list or loop of curves                */
   struct ag_crvs       *next;  /* next      in list               */
   struct ag_crvs       *prev;  /* prev      in list               */
   struct ag_curve      *crv;   /* curve                           */
   } AG_CRVS, *AG_CRVSP;

typedef struct ag_curve    {    /* curve data structure            */
    int               dim;      /* dimension 2 or 3                */
    int               nbs;      /* number of b-splines             */
    int               form;     /*   0(open)       1 (closed)      */
                                /*   2(periodic)  -1 (disjoint)    */
    struct ag_spline  *bs0;     /* first in loop of b-splines      */
    struct ag_spline  *bs;      /* current b-spline                */
    struct ag_mmbox   *cbox;    /* min-max box of curve            */
    } AG_CURVE, *AG_CURVEP;
 
typedef struct ag_spline   {    /* b-spline data structure         */
    struct ag_spline  *next;    /* next b-spline in loop           */
    struct ag_spline  *prev;    /* previous b-spline in loop       */
    int               ctype;    /* code for type of curve          */
    int               dim;      /* dimension                       */
    int               m;        /* degree                          */
    int               n;        /* number of spans                 */
    int               rat;      /* 0(nonrat) 1(rational) -1(homo)  */
    int               form;     /* 0(open)  1(closed)  2(periodic) */
    struct ag_cnode   *node0;   /* node for knot t0 in cnode list  */
    struct ag_cnode   *noden;   /* node for knot tn                */
    struct ag_cnode   *node;    /* current node                    */
    struct ag_mmbox   *bsbox;   /* min-max box of b-spline         */
    } AG_SPLINE, *AG_SPLINEP;
 
typedef struct ag_cnode    {    /* curve node data structure       */
    struct ag_cnode   *next;    /* next curve node                 */
    struct ag_cnode   *prev;    /* previous curve node             */
    double            *Pw;      /* control point and weight        */
    double            *t;       /* knot                            */
    } AG_CNODE, *AG_CNODEP;


/* *** BOX DATA *** */
typedef struct ag_mmbox    {    /* min-max box data structure      */
    double            *min;     /* minimum (pointer to array)      */
    double            *max;     /* maximum (pointer to array)      */
    } AG_MMBOX, *AG_MMBOXP;



