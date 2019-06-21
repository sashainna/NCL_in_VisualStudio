/* NAME :  ag_globinit.h      MODULE : Global Constants 
** VERSION : 0.5              DATE LAST MODIFIED : 02/01/88
** CONTAINS: Global constant initialisation
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ag_globinit.h , 23.1
**    DATE AND TIME OF LAST MODIFICATION
**       05/22/12 , 11:18:56
********************************************************************/
                         
/* *** for longjmp error return  *** */
#include "setjmp.h"
jmp_buf AG_error_stack;  

/* *** display surface draw between knot u,v *** */
int AG_srf_drwu = 1;       
int AG_srf_drwv = 1;       

/* *** memory page allocation *** */
/* AG_mem_page (T/F) allocate memory by pages, deallocate whole
   pages if false then allocate/deallocate each size as needed  */
int AG_mem_page  = FALSE;
                                                             
/* *** tolerances *** */

#if UU_COMP != UU_IRIS
double  AG_tol_dist   = 1.0E-6;      
double  AG_tol_dist2  = 1.0E-12;     
double  AG_tol_dist10 = 1.0E-5;      
double  AG_tol_ortho  = 1.0E-10;     
double  AG_tol_ortho2 = 1.0E-20;    
double  AG_tol_knot   = 1.0E-10;      
double  AG_tol_knot2  = 1.0E-20;     
double  AG_tol_mach   = 1.0E-13;
double  AG_tol_mach10 = 1.0E-12;    
double  AG_tol_cfit   = 0.01;      
double  AG_tol_sfit   = 1.0E-10;
#endif
#if UU_COMP == UU_IRIS 
#ifdef UU_DOUBLE
double  AG_tol_dist   = 1.0E-6;      
double  AG_tol_dist2  = 1.0E-12;     
double  AG_tol_dist10 = 1.0E-5;      
double  AG_tol_ortho  = 1.0E-10;     
double  AG_tol_ortho2 = 1.0E-20;    
double  AG_tol_knot   = 1.0E-10;      
double  AG_tol_knot2  = 1.0E-20;     
double  AG_tol_mach   = 1.0E-13;
double  AG_tol_mach10 = 1.0E-12;    
double  AG_tol_cfit   = 0.01;      
double  AG_tol_sfit   = 1.0E-10;
#endif
#endif
#if UU_COMP == UU_IRIS 
#ifdef UU_SINGLE
double  AG_tol_dist   = 1.0E-4;      
double  AG_tol_dist2  = 1.0E-8;     
double  AG_tol_dist10 = 1.0E-3;      
double  AG_tol_ortho  = 1.0E-4;     
double  AG_tol_ortho2 = 1.0E-8;    
double  AG_tol_knot   = 1.0E-4;      
double  AG_tol_knot2  = 1.0E-8;     
double  AG_tol_mach   = 1.0E-6;
double  AG_tol_mach10 = 1.0E-5;    
double  AG_tol_cfit   = 0.01;      
double  AG_tol_sfit   = 1.0E-4;
#endif
#endif

/* binomial coefficients and pointer  tables */
int  AG_binom_table[(AG_MAXORD+1)*(AG_MAXORD+2)/2];
int *AG_binom[AG_MAXORD];      

/* *** error value *** */
int AG_error = 0;
 
/* *** display test of fit to tolerance *** */
int AG_test_fit = FALSE;

/* *** surface/surface intersection points *** */
int AG_ssint_pr = 0;
                                                                 
/* *** user dialog *** */
int AG_tnsh_print = FALSE;   /* print data                         */  
int AG_tnsh_fcurve = FALSE;  /* display face/curve intersections   */
int AG_menu_print = 1;       /* print menu is on                   */
int AG_str_tr  = 0;          /* do not ask to transform input      */ 
int AG_spec_bypass = 0;      /* T = bypass special case  intersect */
int AG_rbf_chk_fit = 0;      /* check fit flag for rbf             */
int AG_rbf_n_chk_pts = 0;    /* number of check points in rbf      */
int AG_spandot = 1;          /* display dot ant span boundary      */
int AG_graphics_mode = 0;    /* graphics mode AG=0, user =1        */
int AG_display_mode = 0;     /* direct = 0 borrow = 1              */
int AG_scn_over = 0;         /* object to screen overlap           */

