/* NAME :  ag_crv_iter.h      MODULE : curves internal
** VERSION : 0.5              DATE LAST MODIFIED : 02/01/88
** CONTAINS: General iteration working structures for curves 
**  ag_crv_iter_dat      general iteration data for curves
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/

typedef struct ag_crv_iter_dat { /* curve general iteration data   */
   double                check_value;
   int                   error;
   int                   nbs, ncv;
   AG_SPLINEP            *bs;
   AG_CURVEP             *cv;
   int                   *clbs, *clcv;
   double                *tbs,  *tcv;
   double                *gtbs, *gtcv;
   int                   (*iterate)();
   int                   (*check)();
   char                  *par;
   int                   nit;
   int                   done; 
   } AG_CRV_ITER_DAT, *AG_CRV_ITER_DATP;

