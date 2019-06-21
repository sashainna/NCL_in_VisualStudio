/* NAME :  ag_gen_iter.h      MODULE : Utility internal
** VERSION : 0.5              DATE LAST MODIFIED : 02/01/88
** CONTAINS: General iteration working structures
**  ag_gen_iter_dat   general iteration data 
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/

typedef struct ag_gen_iter_dat { /* general iteration data         */
   double                check_value;
   int                   error;
   int                   nbs, ncv, nsrf;
   AG_SPLINEP            *bs;
   AG_CURVEP             *cv;
   AG_SURFACEP           *srf;
   int                   *clbs, *clcv, *clusrf, *clvsrf;
   double                *tbs,  *tcv,  *usrf,  *vsrf;
   double                *gtbs, *gtcv, *gusrf, *gvsrf;
   int                   (*iterate)();
   int                   (*check)();
   char                  *par;
   int                   nit;
   int                   done; 
   } AG_GEN_ITER_DAT, *AG_GEN_ITER_DATP;

