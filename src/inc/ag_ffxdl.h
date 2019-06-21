/* NAME :  ag_ffxdl.h        MODULE : Solid internal
** VERSION : 0.5             DATE LAST MODIFIED : 02/01/88
** CONTAINS:  face/face intersection data
**  ag_ffxdh       face/face intersection header
**  ag_fAfxdl      faceA/face int data loop 
**  ag_fAfBxdl     faceA/faceB int data loop  
**  ag_ffxd_cl     int component loop     
**  ag_pVdata      int data for a face   
**  ag_ffxdl       face/face int point loop  
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/

typedef struct ag_ffxdh {      /* face/face intersection header    */
   struct ag_fAfxdl  *fAfxd0;  /* first fA data of loop            */
   } AG_FFXDH, *AG_FFXDHP;

typedef struct ag_fAfxdl {     /* faceA/face int data loop         */
   struct ag_fAfxdl  *next;    /* ordered by faceA ptr             */
   struct ag_fAfxdl  *prev;
   struct ag_face    *fA;
   struct ag_fAfBxdl *fAfBxd0; /* first faceA/faceB data           */
   } AG_FAFXDL, *AG_FAFXDLP;

typedef struct ag_fAfBxdl {    /* faceA/faceB int data loop        */
   struct ag_fAfBxdl *next;    /* ordered by fB                    */
   struct ag_fAfBxdl *prev;
   struct ag_face    *fB;
   struct ag_ffxd_cl *ffxd_c0; /* first int component of loop      */
   } AG_FAFBXDL, *AG_FAFBXDLP;
                             
typedef struct ag_ffxd_cl {    /* int component loop               */
   struct ag_ffxd_cl *next;    /* ordered by c_id                  */
   struct ag_ffxd_cl *prev;
   int               c_id;     /* component id                     */
   int               Vorder;   /* data ordered by position (!=0)   */
   struct ag_ffxdl   *ffxd0;   /* first int data of loop           */
   } AG_FFXD_CL, *AG_FFXD_CLP;

typedef struct ag_pVdata {     /* int data for a face              */
   struct ag_face    *f;
   double            pV[2];    /* 2-d pt for pe                    */
   double            dpV[2];   /* 2-d direction for pe             */
   int               reve;     /* T or F e is reverse of pe        */
   int               ps_e_i;   /* pV start (-1),inter (0), end (1) */
   int               eonf[2];  /*[0] start  1=onf, 2=oef, 0=else   */
                               /*[1] end    1=onf, 2=oef, 0=else   */
   int               at_Vert;  /*  -1=start,  1=end,  0=else       */
   struct ag_tedge   *rte;     /* ref te, V at end of e            */
                               /*   unless at_Vert != 0            */
   } AG_PVDATA, *AG_PVDATAP;

typedef struct ag_ffxdl {      /* face/face int point loop         */
   struct ag_ffxdl   *next;    /* may be ordered by position       */
   struct ag_ffxdl   *prev;
   int               ff_tan;   /* faces tangent(Na*Nb=+-1) else(0) */
   int               V_id;     /* point identifier                 */
   double            V[3];     /* 3-d point for e                  */
   double            dV[3];    /* 3-d direction for e              */
   struct ag_pVdata  pVfA;     /* int point data for fA            */
   struct ag_pVdata  pVfB;     /* int point data for fB            */
   } AG_FFXDL, *AG_FFXDLP;


