/* NAME :  ag_lr_extr.h       MODULE : Solid utility
** VERSION : 0.9              DATE LAST MODIFIED : 09/05/88
** CONTAINS: Line radius extrusion data
**  ag_lr_extr_h    line_radius extrude header
**  ag_Pbl_Prd      point, radius data loop
Copyright (c) 1988 Applied Geometry Corporation. All rights reserved.
********************************************************************/


typedef struct ag_lr_extr_h {    /* line-radius extrude header     */
   double                  z0;   /* z0 z value at origin           */
   double                   L;   /* length for extrusion           */
   struct ag_lr_extr_dl *P_r0;   /* ptr into point, radius loop    */
   } AG_LR_EXTR_H, *AG_LR_EXTR_HP;

typedef struct ag_lr_extr_dl {   /* point, radius data loop        */
   struct ag_lr_extr_dl *next;
   struct ag_lr_extr_dl *prev;
   double                P[2];   /* point (2d)                     */
   double              radius;   /* radius                         */
   } AG_LR_EXTR_D, *AG_LR_EXTR_DP;

