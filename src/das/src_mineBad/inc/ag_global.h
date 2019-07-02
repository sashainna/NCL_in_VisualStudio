/* NAME :  ag_global.h        MODULE : Global constants 
** VERSION : 0.5              DATE LAST MODIFIED : 02/01/88
** CONTAINS: Global value declarations
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/

#include "setjmp.h"
extern jmp_buf AG_error_stack;  
extern int AG_srf_drwu;           /* surface draw u                */
extern int AG_srf_drwv;           /* surface draw v                */
extern int AG_mem_page;           /* page allocate (T) or not(F)   */
extern double  AG_tol_dist;      /* distance                       */
extern double  AG_tol_dist2;     /* distance squared               */
extern double  AG_tol_dist10;    /* 10 x distance                  */
extern double  AG_tol_ortho;     /* orthogonal                     */
extern double  AG_tol_ortho2;    /* orthogonal squared             */
extern double  AG_tol_knot;      /* knot                           */
extern double  AG_tol_knot2;     /* knot squared                   */
extern double  AG_tol_mach;      /* machine                        */
extern double  AG_tol_mach10;    /* machine*10                     */
extern double  AG_tol_cfit;      /* curve fit                      */
extern double  AG_tol_sfit;      /* surface fit                    */
extern int *AG_binom[AG_MAXORD]; /* binomial coefficient pointers  */
extern int AG_error;              /* error return number           */
extern int AG_test_fit;           /* diagnostic print curve fit    */
extern int AG_ssint_pr;           /* print surface/surface  int    */
extern int AG_tnsh_print;         /* diagnostic print boolean shell*/
extern int AG_tnsh_fcurve;        /* display face/curve inters     */
extern int AG_menu_print;         /* print menu options            */
extern int AG_str_tr;             /* ask to transform input        */
extern int AG_spec_bypass;        /* if T bypass special case inter*/
extern int AG_rbf_chk_fit;        /* check fit flag for rbf        */
extern int AG_rbf_n_chk_pts;      /* number of check points in rbf */
extern int AG_spandot;            /* display dot at span boundaries*/
extern int AG_display_mode;       /* direct = 0, borrow = 1        */
extern int AG_scn_over;           /* object screen overlap         */

