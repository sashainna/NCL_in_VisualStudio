/* NAME :  ag_constant.h      MODULE : Constants 
** VERSION : 0.8              DATE LAST MODIFIED : 05/11/88
** CONTAINS: Constant definifitions
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/

/* *** file formats *** */
#define AG_ASCII           123
#define AG_BINARY          795
 
/* *** curve types *** */
#define AG_OTHER           0
#define AG_LINE            1
#define AG_PARABOLA        2
#define AG_CIRCLE          3
#define AG_ELLIPSE         4
#define AG_HYPERBOLA       5

/* *** curve or surface types *** */
#define AG_POWER         101
#define AG_POWER01       102
#define AG_BEZIER        103
#define AG_BEZIER01      104
 
/* *** surface types *** */
#define AG_PLANE           1
#define AG_CYLINDER        2
#define AG_CONE            3
#define AG_SPHERE          4
#define AG_TORUS           5
#define AG_SRF_REV         6

/* *** fillet types *** */
#define AG_RB_FIL_CYL    101 
#define AG_RB_FIL_FPL    102
#define AG_RB_FIL_FF     103
#define AG_RB_CFIL_TOR   111
#define AG_RB_CFIL_EPL   112
#define AG_RB_CFIL_EF    113

/* *** set the null flag if not already set *** */
#ifndef NULL
/* cast null as 32 bit (pointer length or char *) integer zero */ 
#define NULL (long)0
#endif
#define TRUE   1
#define FALSE  0  
 
/* *** set degree and dimension bounds *** */
#define AG_MAXDEG    (5)
#define AG_MAXORD    (AG_MAXDEG + 1)
#define AG_MAXDIM    (3)
#define AG_MAXHDIM   (AG_MAXDIM + 1)
#define AG_MAXITER   (10)
 
/* *** set an error flag *** */
#define AG_ERR           (-1)
#define AG_ERR_WARNING     1      
#define AG_ERR_SEVERE      2    
#define AG_ERR_FATAL       3     
#define AG_ERR_TRACE       0     /* Error in lower level routine */
#define AG_ERR_OVERFLOW   901    /* Too Many Errors */           
#define AG_ERR_ALLOC      902    /* Memory allocation */         
#define AG_ERR_READ       903    /* Read error (file) */         
#define AG_ERR_DATA       904    /* Invalid data value */         
#define AG_ERR_FOPEN      905    /* File open error */            
#define AG_ERR_RANGE      906    /* Number out of range */       
#define AG_ERR_DEALLOC    907    /* Memory de-allocation error*/
#define AG_ERR_STRUCT     908    /* Structure pointer error */ 
#define AG_ERR_ITER       909    /* Iteration fails to converge */
#define AG_ERR_MISSING    910    /* Missing algorithm for this*/
