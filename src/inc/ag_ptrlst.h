/* NAME :  ag_ptrlst.h        MODULE : Utility internal
** VERSION : 0.5              DATE LAST MODIFIED : 02/01/88
** CONTAINS: List of pointers, used for surface i/o
**  ag_ptrlsth     pointer list data 
**  ag_ptrlst      pointer list data
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/

typedef struct ag_ptrlsth {  /* pointer list data                  */
   struct ag_ptrlst   *L0;
   } AG_PTRLSTH, *AG_PTRLSTHP;

typedef struct ag_ptrlst {   /* pointer list data                  */
   char               *P;
   struct ag_ptrlst   *next;
   } AG_PTRLST, *AG_PTRLSTP;
 
