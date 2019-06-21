/* NAME :  ag_efxdel.h        MODULE : Solid  internal
** VERSION : 0.5              DATE LAST MODIFIED : 02/01/88
** CONTAINS:  edge/face intersection data
**  ag_e_ped       edge/pedge    data  
**  ag_efxdelh     edge/face int data edge loop header 
**  ag_efxdel      edge/face int data edge loop
**  ag_efxdpl      edge/face int data point loop  
**  ag_efxdl       edge/face int data loop
**  ag_efxdVl      edge/face int data vertex loop  
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/

typedef struct ag_e_ped {       /* edge/pedge data                 */
   struct ag_tedge     *te;     /* tedge of edge                   */
   struct ag_spline    *ebs;    /* edge bs                         */
   double              et;      /* parameter for ebs               */
   double              pV0[2];  /* u,v on face owned by te         */
   double              pV1[2];  /* u,v on face owned by twin       */
   struct ag_spline    *pe0bs;  /* pedge bs for te                 */
   struct ag_spline    *pe1bs;  /* pedge bs for twin               */
   double              pe0t;    /* parameter for pe0bs             */
   double              pe1t;    /* parameter for pe1bs             */
   } AG_E_PED, *AG_E_PEDP;

typedef struct ag_efxdelh {     /* edge/face int edge loop header  */
   struct ag_efxdel    *efxde0; /* initial edge loop               */
   struct ag_efxdVl    *efxdV0; /* initial V loop                  */
   } AG_EFXDELH, *AG_EFXDELHP; 

typedef struct ag_efxdel {      /* edge/face int data edge loop    */
   struct ag_efxdel    *next;   /* ordered by edge of tedge        */
   struct ag_efxdel    *prev;
   struct ag_tedge     *te;     /* tedge                           */
   struct ag_efxdpl    *efxdp0; /* initial data point              */
   } AG_EFXDEL, *AG_EFXDELP;

typedef struct ag_efxdpl {      /* edge/face int data point loop   */
   struct ag_efxdpl    *next;   /* ordered by position on edge     */
   struct ag_efxdpl    *prev;
   int                 V_id;    /* intersection point identifier   */
   double              V[3];    /* x,y,z of intersection           */
   struct ag_e_ped     eped;    /* edge/pedge data                 */
   int                 at_Vert; /* V is at -1=start 
                                     0=interior, 1=end of e        */
   int                 fxtype;  /* V is at 0=interior of f         */
                                /*   1=interior of e of f          */
                                /*   2=start or 3=end of e of f    */
   struct ag_tedge     *rte;    /* refined te with V at end of     
                                     e of rte unless at_Vert != 0  */
   struct ag_efxdl     *efxd0;  /* face data                       */
   } AG_EFXDPL, *AG_EFXDPLP;

typedef struct ag_efxdl {       /* edge/face int data loop         */
   struct ag_efxdl     *next;
   struct ag_efxdl     *prev;
   struct ag_face      *f;
   double              pV[2];   /* u,v param point on f            */
   int                 eonf[2]; /* [0] start 1=onf, 2=oef, 0 =else   
                                   [1] end   1=onf, 2=oef, 0 =else */
   struct ag_tedge     *te;    /* tedge containing edge(or NULL)   */
   double              et;     /* tedge(et) = int point            */
   } AG_EFXDL, *AG_EFXDLP;

typedef struct ag_efxdVl {      /* edge/face int data vertex loop  */
   struct ag_efxdVl    *next;   /* not ordered                     */
   struct ag_efxdVl    *prev;
   int                 V_id;    /* vertex identifier               */
   double              V[3];    /* vertex at intersection          */
   struct ag_efxdel    *efxde0; /* initial ordered te of loop at V */
   } AG_EFXDVL, *AG_EFXDVLP;

/*
Notes: The original tedge gets divided at each distinct interior
       point (at_Vert = 0). V is at the end of e of rte after 
       this division.
       If at_Vert != 0 then V is at the start (at_Vert < 0) or end
       (at_Vert > 0) of e of rte.
       rte is used for connecting the new tedges, built at the
       intersections between faces, to the original tedges.
       ag_efxdVl is built by going around the vertex so ag_efxdVl 
       must be built before the tedges are refined.
       efxde of ag_efxdVl are ordered by rotation about V.
       in efxdl:
         te is NULL if point not on edge of face
         if te != NULL then te(et) is point on edge
*/
