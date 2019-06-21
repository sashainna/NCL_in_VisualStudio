/* NAME :  ag_ftedata.h       MODULE : Solid internal
** VERSION : 0.5              DATE LAST MODIFIED : 02/01/88
** CONTAINS: fit twinedge working data
**  ag_ftedata     fit tedge data
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/
                                                                
typedef struct ag_ftedata {     /* fit tedge data                  */
   double            fit_e;     /* edge fit tolerance              */ 
   double            fit_pe[2]; /* pedge fit tolerance             */
   int               reve[2];   /* (1)=parameter direction reversed*/
   double            e_er2;     /* edge error squared              */
   double            pe_er2[2]; /* pedge error squared             */
   double            stol;      /* stopping tolerance              */
   double            fitol2;    /* fit tolerance                   */
   struct ag_face    *fA;       /* face A                          */
   struct ag_face    *fB;       /* face B                          */
   } AG_FTEDATA, *AG_FTEDATAP;

/*
 Notes: These structures are used for fitting a new tedge
        to a face/face or face/plane intersection.
        sP of ag_srfdata may have sP->P = S, sP->unext->P = Su
        and sP->vnext->P = Sv.
*/
