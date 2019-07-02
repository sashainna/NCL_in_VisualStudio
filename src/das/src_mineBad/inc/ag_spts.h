/* NAME :  ag_spts.h          MODULE : Surface internal
** VERSION : 0.8              DATE LAST MODIFIED : 04/26/88
** CONTAINS: Surface point groupings
**  ag_spts1      surface points for a single derivative
**  ag_spts2      surface points for a single derivative
**  ag_spts3      surface points for a single derivative
Copyright (c) 1988 Applied Geometry Corporation. All rights reserved.
********************************************************************/

typedef struct ag_spts1 {  /* surface points for one derivative */
   double            S00[3],S10[3],S01[3];
   struct ag_spoint  sP00,sP10,sP01;
   } AG_SPTS1, *AG_SPTS1P;


typedef struct ag_spts2 {   /* surface points for two derivatives */
   double            S00[3],S10[3],S01[3],S20[3],S11[3],S02[3];
   struct ag_spoint  sP00,sP10,sP01,sP20,sP11,sP02;
   } AG_SPTS2, *AG_SPTS2P;


typedef struct ag_spts3 {   /* surface points for three derivatives */
   double            S00[3],S10[3],S01[3],S20[3],S11[3],S02[3],
                     S30[3],S21[3],S12[3],S03[3];  
   struct ag_spoint  sP00,sP10,sP01,sP20,sP11,sP02,
                     sP30,sP21,sP12,sP03;
   } AG_SPTS3, *AG_SPTS3P;
