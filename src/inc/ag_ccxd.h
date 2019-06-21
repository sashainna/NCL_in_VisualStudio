/* NAME :  ag_ccxd.h          MODULE : Intersection curve/curve 
** VERSION : 0.5              DATE LAST MODIFIED : 02/01/88
** CONTAINS: curve/curve intersection structures
**  ag_ccxh        curve/curve  intersection header  
**  ag_ccxd        curve/curve  intersection data
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/

typedef struct ag_ccxh {          /* crv/crv intersection header   */
  struct ag_curve     *c0, *c1;   /* the two curves to intersect   */
  struct ag_ccxd      *ccxd0;     /* initial data element          */
  } AG_CCXH, *AG_CCXHP;
 
typedef struct ag_ccxd {          /* curve/curve intersect record  */
  struct ag_ccxd      *next;                                       
  struct ag_ccxd      *prev;      /* n & p members of data loop    */
  double              t0, t1;     /* intersection parameter values */
  double              V[3];       /* point in real space           */
  int                 c_on_c;     /* overlap  TRUE or FALSE        */
  } AG_CCXD, *AG_CCXDP;
 
/*
Notes: If c_on_c is true then this point is the start of an
       interval of c0 which overlaps c1.     The end of the 
       interval is the next ccxdata point.
*/
 
