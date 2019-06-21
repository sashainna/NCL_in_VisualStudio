/* NAME :  ag_srfdata.h       MODULE : Surface internal
** VERSION : 0.5              DATE LAST MODIFIED : 02/01/88
** CONTAINS: Surface evaluation
**  ag_srfdata     surface evaluation data 
**  ag_srfdata2    surface evaluation data ( 2 derivatives )
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/

typedef struct ag_srfdata {     /* surface evaluation data         */
   struct ag_surface *srf;      /* surface                         */
   double            *uv;       /* u,v array                       */
   double            *S;        /* surface values                  */
   double            *Su;       /* dS/du                           */
   double            *Sv;       /* dS/dv                           */
   double            *N;        /* Normal                          */
   struct ag_spoint  *sP;       /* spoint list for surface values  */
   struct ag_mmbox   *pbox;     /* mmbox (may be null)             */
   } AG_SRFDATA, *AG_SRFDATAP;
                                                                
typedef struct ag_srfdata2 {    /* surface evaluation data 2 ders  */
   struct ag_surface *srf;      /* surface                         */
   double            *uv;       /* u,v array                       */
   double            *S;        /* surface values                  */
   double            *Su;       /* dS/du                           */
   double            *Sv;       /* dS/dv                           */
   double *Suu,*Suv,*Svv;       /* 2nd order partials              */
   double            *N;        /* Normal                          */
   struct ag_spoint  *sP;       /* spoint array for surface values */
   struct ag_mmbox   *pbox;     /* mmbox (may be null)             */
   } AG_SRFDATA2, *AG_SRFDATA2P;

