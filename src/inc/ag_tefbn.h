/* NAME :  ag_tefbn.h         MODULE : Solid  internal
** VERSION : 0.5              DATE LAST MODIFIED : 02/01/88
** CONTAINS: twin edge, face, and boundary list data
**  ag_telisth     twin edge list header 
**  ag_telist      twinedge record   
**  ag_flisth      face list header   
**  ag_flist       face list data 
**  ag_bndryh      boundary header  
**  ag_bnest       nested boundary data       

*  ag_tesl        new tedge segment list
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/

typedef struct ag_telisth {      /* twin edge list header          */ 
   struct ag_telist       *tel0;
   } AG_TELISTH, *AG_TELISTHP;

typedef struct ag_telist {       /* twinedge record                */
   struct ag_telist       *next; /* loop of twin edges             */
   struct ag_telist       *prev; 
   struct ag_tedge        *te0;
   } AG_TELIST, *AG_TELISTP;

typedef struct ag_flisth {       /* face list header               */
   struct ag_flist        *fl0;
   } AG_FLISTH, *AG_FLISTHP;

typedef struct ag_flist {        /* face list data                 */
   struct ag_flist        *next; /* loop of faces                  */
   struct ag_flist        *prev;    
   struct ag_face         *f;
   } AG_FLIST, *AG_FLISTP;

typedef struct ag_bndryh {       /* boundary header                */
   struct ag_boundary     *b0;   /* first boundary of list         */
   } AG_BNDRYH, *AG_BNDRYHP;

typedef struct ag_bnest {        /* nested boundary data           */
   struct ag_bnest        *next;     
   struct ag_bnest        *prev;
   int                    outer; /* outer  boundary        T or F */
   struct ag_boundary     *b;
   struct ag_bnest        *lower;
   } AG_BNEST, *AG_BNESTP;

