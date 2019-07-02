/* NAME :  ag_vsvl.h          MODULE : Solid internal
** VERSION : 0.5              DATE LAST MODIFIED : 02/01/88
** CONTAINS: V segment loop data
**  ag_Vseglh      V segment loop header
**  ag_Vsegl       V seg loop  
**  ag_Vlooph      V loop header     
**  ag_Vloop       V loop data 
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/

typedef struct ag_Vseglh {   /* V segment loop header              */
   int               V_id; 
   int               nseg;
   struct ag_Vsegl   *Vseg0; /* start of loop                      */
   } AG_VSEGLH, *AG_VSEGLHP;

typedef struct ag_Vsegl {    /* V seg loop                         */
   struct ag_Vsegl   *next;  /* ordered by direction               */
   struct ag_Vsegl   *prev;     
   int               i0;     /* at V0 (0) or V1 (1)                */
   struct ag_tesl    *tes;
   } AG_VSEGL, *AG_VSEGLP;

typedef struct ag_Vlooph {   /* V loop header                      */
   struct ag_Vloop   *Vl0;   /* start of loop                      */
   } AG_VLOOPH, *AG_VLOOPHP;

typedef struct ag_Vloop {    /* V loop data                        */
   struct ag_Vloop   *next;  /* V loop                             */
   struct ag_Vloop   *prev;    
   int               V_id;
   } AG_VLOOP, *AG_VLOOPP;
