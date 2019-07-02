/* NAME :  ag_bs_pt.h         MODULE : Curve definition
** VERSION : 0.5              DATE LAST MODIFIED : 02/01/88
** CONTAINS: list of points along  a b-spline 
**  ag_bs_pt       points along a bspline structure
**  ag_bl_Prd      blend, point, radius data 
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/

typedef struct ag_bs_pt { /* points along bspline structure        */
    struct ag_bs_pt     *next, *prev;
    struct ag_spline    *bs;
    int                 id;
    struct ag_cnode     *node;
    double              t;
    }  AG_BS_PT, *AG_BS_PTP;

