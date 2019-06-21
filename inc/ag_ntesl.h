/* NAME :  ag_ntesl.h         MODULE : Solid internal
** VERSION : 0.5              DATE LAST MODIFIED : 02/01/88
** CONTAINS: New twinedge list data
**  ag_ntelh       new tedge list header
**  ag_ntel        new te list  
**  ag_tesl        new tedge segment list
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/

typedef struct ag_ntelh {       /* new tedge list header           */
   struct ag_ntel     *nte0;    /* start new tedge                 */
   struct ag_ntel     *nte1;    /* end   new tedge                 */
   struct ag_ntel     *nte;     /* current new tedge               */
   } AG_NTELH, *AG_NTELHP;

typedef struct ag_ntel  {       /* new tedge list                  */
   struct ag_ntel     *next;    /* ordered by face ptr             */
   struct ag_ntel     *prev;
   struct ag_face     *f;       /* face                            */
   struct ag_tesl     *tes0;    /* ptr to initial tedge segment    */
   } AG_NTEL, *AG_NTELP;

typedef struct ag_tesl  {       /* new tedge segment list          */
   struct ag_tesl     *next;
   struct ag_tesl     *prev;
   int                V_id0;    /* id at start of segment          */
   int                V_id1;    /* id at end   of segment          */
   struct ag_tedge    *te0;     /* initial  tedge of seg           */
   struct ag_tedge    *te1;     /* final    tedge of seg           */
   } AG_TESL, *AG_TESLP;

/*
Notes: V_id0 or V_id1 are "dangling" start or end points if te0->prev
       or te1->next is null. te0->prev and te1->next are the
       connections into the original tedges.
*/
